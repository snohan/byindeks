# City yearly index for cities using toll stations
# Binds TRP and toll station index
# Calculates city index

# IN
# TRP index monthly
# Toll station index monthly

# OUT
# City index
# Excel file

if(city_number == 960) {

  city_name <- "Trondheim"

  trp_index_20 <- get_published_pointindex_for_months_toll_cities(city_number, 2020, 12)
  trp_index_21 <- get_published_pointindex_for_months_toll_cities(city_number, 2021, 12)
  trp_index_22 <- get_published_pointindex_for_months_toll_cities(city_number, 2022, 12)
  trp_index_23 <- get_published_pointindex_for_months_toll_cities(city_number, 2023, 12)
  trp_index_24 <- get_published_pointindex_for_months_toll_cities(city_number, 2024, 12)
  trp_index_25 <- get_published_pointindex_for_months_toll_cities(city_number, 2025, 12)
  trp_index_26 <- get_published_pointindex_for_months_toll_cities(city_number, 2026, index_month)

  trp_index_all <-
    dplyr::bind_rows(
      trp_index_20[[2]],
      trp_index_21[[2]],
      trp_index_22[[2]],
      trp_index_23[[2]],
      trp_index_24[[2]],
      trp_index_25[[2]],
      trp_index_26[[2]]
    )

  toll_index_yearly_raw <- readr::read_rds("H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser.rds")
  toll_index_monthly_raw <- readr::read_rds("H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser.rds")
}

if(city_number == 19955) {

  city_name <- "Haugesund"

  trp_index_26 <- get_published_pointindex_for_months_toll_cities(city_number, 2026, index_month)

  trp_index_all <-
    dplyr::bind_rows(
      trp_index_26[[2]]
    )

  toll_index_yearly_raw <- readr::read_rds("H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_aarsindekser_haugesund.rds")
  toll_index_monthly_raw <- readr::read_rds("H:/Programmering/R/byindeks/data_indexpoints_tidy/bom_maanedsindekser_haugesund.rds")
}
# Raw data from AutoPASS has trp_id = autopass_id


# TRP index yearly ----
trp_toll_index_yearly <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE
  ) |>
  # Need latest value per year
  dplyr::filter(
    period == "year_to_date"
    # month == max(month)
  ) |>
  dplyr::slice_max(month, by = c(trp_id, year)) |> 
  dplyr::select(
    trp_id,
    year,
    month,
    length_range,
    base_volume,
    calc_volume,
    coverage,
    index
  ) |>
  dplyr::mutate(
    # month = lubridate::make_date(year = year, month = month)
    # In case some TRPs doesn't have December as latest month, and for the subsequent grouping to work as intended,
    # name all months per year as December here for the yearly index.
    # month = lubridate::make_date(year = year, month = 12),
    month = dplyr::case_when(
      year < present_year ~ lubridate::make_date(year = year, month = 12),
      TRUE ~ lubridate::make_date(year = year, month = month)
    )
  ) |>
  dplyr::bind_rows(
    toll_index_yearly_raw |>
      dplyr::rename(
        index = index_p,
        length_range = class
      ) |>
      dplyr::filter(length_range != "unknown")
  )

 trp_toll_index_yearly_short <-
  trp_toll_index_yearly |>
  dplyr::filter(length_range == "lette") |>
  dplyr::select(trp_id, year, index) |>
  dplyr::mutate(index = base::round(index, 2)) |> 
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "index_",
    values_from = index
  )


# Binding pointindices to all points
this_citys_trps_all_adt_final_index <-
  this_citys_trps_all_adt_final |>
  dplyr::left_join(
    trp_toll_index_yearly_short,
    by = dplyr::join_by(autopass_id == trp_id)
  ) |>
  split_road_system_reference()

# Delete?
# trp_index_refyear <-
#   this_citys_trps_all_adt_final_index |>
#   dplyr::select(
#     trp_id,
#     tidyselect::starts_with("index")
#   ) |>
#   dplyr::filter(
#     dplyr::if_all(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ !is.na(.x)
#     )
#   ) |>
#   dplyr::mutate(
#     dplyr::across(
#       .cols = tidyselect::starts_with("index"),
#       .fns = ~ index_converter(.))
#   ) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(index = prod(c_across(tidyselect::starts_with("index")))) |>
#   dplyr::mutate(index = round(100 * (index - 1), digits = 2)) |>
#   dplyr::select(trp_id, index)

# this_citys_trp_index_refyear <-
#   this_citys_trps_all_adt_final_index |>
#   dplyr::left_join(
#     trp_index_refyear,
#     by = "trp_id"
#   )

# readr::write_rds(
#   this_citys_trp_index_refyear,
#   file = paste0("data_indexpoints_tidy/indekspunkt_", city_number, ".rds")
# )


# City index yearly----
city_index_yearly <-
  trp_toll_index_yearly |>
  dplyr::filter(!is.na(index)) |>
  dplyr::group_by(
    length_range,
    year,
    month
  ) |>
  calculate_area_index() |>
  dplyr::mutate(
    year_base = year - 1,
    year_from_to = paste0(year - 1, "-", year),
    index_i = index_converter(index_p),
    month = lubridate::month(month)
  )


# Toy example of the weigths compared to n in calculation of standard error
# toy <-
#   tibble::tibble(
#     weights = c(0.001, 0.001, 0.997, 0.001)
#     #weights = c(0.25, 0.25, 0.25, 0.25)
#   ) |>
#   dplyr::mutate(
#     sum_of_weights = sum(weights),
#     sqrt_of_sum_of_squared_weights = sqrt(sum(weights^2)),
#     one_through_sqrt_of_n = 1 / sqrt(length(weights))
#   )
# Conclusion: The minimum value of sqrt(sum(weights^2)) is when all weights are equal and then it is the same
# as one_through_sqrt_of_n.
# Its maximum value is when one of the weights outweigh all others and its value gets close to 1.


city_index_yearly_light <-
  city_index_yearly |>
  dplyr::ungroup() |>
  dplyr::filter(length_range == "lette") |>
  dplyr::select(-standard_deviation)


## Chaining ----
years_1_2 <-
  city_index_yearly_light |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_3 <-
  dplyr::bind_rows(
    years_1_2,
    dplyr::slice(city_index_yearly_light, 3)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_4 <-
  dplyr::bind_rows(
    years_1_3,
    dplyr::slice(city_index_yearly_light, 4)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_5 <-
  dplyr::bind_rows(
    years_1_4,
    dplyr::slice(city_index_yearly_light, 5)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

years_1_6 <-
  dplyr::bind_rows(
    years_1_5,
    dplyr::slice(city_index_yearly_light, 6)
  ) |>
  calculate_two_year_index() |>
  dplyr::mutate(index_type = "chained")

# Skipping intermediate years, adding just from first to last
city_index_yearly_all <-
  city_index_yearly_light |>
  dplyr::mutate(index_type = "direct") |>
  # dplyr::bind_rows(
  #   years_1_2,
  #   years_1_3,
  #   years_1_4,
  #   years_1_5,
  #   years_1_6
  # ) |>
  dplyr::mutate(
    length_range = "lette",
    year_from_to = paste0(year_base, "-", year),
    area_name = city_name,
    month_name_short = lubridate::month(month, label = TRUE),
    period = paste0("jan-", month_name_short),
    #ci_lower = round(index_p + stats::qt(0.025, n_trp) * standard_error, 1),
    #ci_upper = round(index_p - stats::qt(0.025, n_trp) * standard_error, 1),
    ci_lower = round(index_p - 1.96 * standard_error, 2),
    ci_upper = round(index_p + 1.96 * standard_error, 2),
    index_p = base::round(index_p, 2),
    index_i = base::round(index_i, 2),
    standard_error = base::round(standard_error, 2)
  )

readr::write_rds(
  city_index_yearly_all,
  file = paste0("data_indexpoints_tidy/byindeks_", city_number, ".rds")
)


# City index monthly ----
trp_index_monthly <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month)
  ) |>
  dplyr::select(
    trp_id,
    length_range,
    base_volume,
    calc_volume,
    index,
    month_object
  )

toll_index_monthly <-
  toll_index_monthly_raw |>
  dplyr::mutate(
    month_object = lubridate::ymd(month_calc)
  ) |>
  dplyr::select(
    trp_id,
    length_range = class,
    base_volume = monthly_volume_base,
    calc_volume = monthly_volume_calc,
    index = index_p,
    month_object
  ) |>
  dplyr::filter(length_range != "unknown")

trp_toll_index_monthly <-
  toll_index_monthly |>
  dplyr::bind_rows(trp_index_monthly) |>
  dplyr::filter(length_range == "lette") |>
  dplyr::mutate(
    year = lubridate::year(month_object),
    month = lubridate::month(month_object),
    index = base::round(index, 2)
  )

# n_points_per_month <-
#   trp_toll_index_monthly |>
#   dplyr::group_by(
#     year,
#     month
#   ) |>
#   dplyr::summarise(n_trp = n())


# For Excel file
trp_index_monthly_wide <-
  # Exception from naming convention to match same df from other cities
  trp_toll_index_monthly |>
  tidyr::complete(
    trp_id,
    year,
    month
  ) |>
  dplyr::mutate(
    month_label = lubridate::make_date(
      year = 2000,
      month = month,
      day = 1
    ) |>
      lubridate::month(label = TRUE)
  ) |>
  dplyr::select(
    autopass_id = trp_id,
    year,
    month_label,
    index
  ) |>
  tidyr::pivot_wider(
    names_from = "month_label",
    values_from = "index"
  ) |>
  dplyr::right_join(
    this_citys_trps_all_adt_final,
    by = dplyr::join_by(autopass_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    year,
    jan:mar
  ) |>
  dplyr::arrange(road_category_and_number, name, year)

city_index_monthly <-
  trp_toll_index_monthly |>
  dplyr::mutate(year = lubridate::year(month_object)) |>
  dplyr::filter(
    !is.na(index),
    year >= 2020
  ) |>
  dplyr::group_by(
    month_object,
    length_range
  ) |>
  calculate_area_index() |>
  dplyr::mutate(
    area_name = city_name,
    year = lubridate::year(month_object),
    month = lubridate::month(month_object),
    period = "month",
    month_name =
      lubridate::month(month_object, label = TRUE, abbr = FALSE) |>
      stringr::str_to_title(),
    index_p = base::round(index_p, 2),
    standard_deviation = base::round(standard_deviation, 2),
    standard_error = base::round(standard_error, 2)
  )

readr::write_rds(
  city_index_monthly,
  file = paste0("data_indexpoints_tidy/byindeks_maanedlig_", city_number, ".rds")
)


# City index so far ----
# E.g. Q1 chained through all years
trp_index_so_far <-
  trp_index_all |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month",
    month <= index_month
  ) |>
  dplyr::mutate(
    month_object = lubridate::make_date(year = year, month = month)
  ) |>
  dplyr::select(
    trp_id,
    length_range,
    base_volume,
    calc_volume,
    index,
    month_object
  )

# Bind with toll point index and calculate so far index per point
trp_toll_index_so_far <-
  toll_index_monthly |>
  dplyr::mutate(
    month = lubridate::month(month_object)
  ) |>
  dplyr::filter(
    month <= index_month
  ) |>
  dplyr::select(
    -month
  ) |>
  dplyr::bind_rows(trp_index_so_far) |>
  dplyr::filter(
    length_range == "lette"
  ) |>
  dplyr::mutate(
    year = lubridate::year(month_object)
  ) |>
  # Summarise per TRP to get n TRP correct
  dplyr::group_by(
    trp_id,
    year,
    length_range
  ) |>
  dplyr::summarise(
    base_volume = sum(base_volume),
    calc_volume = sum(calc_volume),
    index = (sum(calc_volume) / sum(base_volume) - 1 ) * 100,
    trp_month_object = max(month_object),
    .groups = "drop"
  )

city_index_so_far <-
  trp_toll_index_so_far |>
  dplyr::group_by(
    year,
    length_range
  ) |>
  dplyr::mutate(
    weight = (base_volume / sum(base_volume))
  ) |>
  dplyr::summarise(
    city_base_volume = sum(base_volume),
    city_calc_volume = sum(calc_volume),
    index_p = (city_calc_volume / city_base_volume - 1 ) * 100,
    n_trp = n(),
    standard_deviation = sqrt((1 / (1 - sum(weight^2) )) * sum(weight * (index - index_p)^2) ),
    standard_error = sqrt(sum(weight^2) * standard_deviation^2),
    city_month_object = max(trp_month_object),
    .groups = "drop"
  )|>
  dplyr::select(
    -city_base_volume,
    -city_calc_volume
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    month = lubridate::month(city_month_object),
    index_i = index_converter(index_p)
  ) |>
  dplyr::select(
    year_base,
    year,
    month,
    length_range,
    #month_object = city_month_object,
    n_trp,
    standard_deviation,
    standard_error,
    index_i,
    index_p
  )


## Chaining ----
so_far_years_1_2 <-
  city_index_so_far |>
  calculate_two_year_index()

so_far_years_1_3 <-
  dplyr::bind_rows(
    so_far_years_1_2,
    dplyr::slice(city_index_so_far, 3)
  ) |>
  calculate_two_year_index()

so_far_years_1_4 <-
  dplyr::bind_rows(
    so_far_years_1_3,
    dplyr::slice(city_index_so_far, 4)
  ) |>
  calculate_two_year_index()

so_far_years_1_5 <-
  dplyr::bind_rows(
    so_far_years_1_4,
    dplyr::slice(city_index_so_far, 5)
  ) |>
  calculate_two_year_index()

so_far_years_1_6 <-
  dplyr::bind_rows(
    so_far_years_1_5,
    dplyr::slice(city_index_so_far, 6)
  ) |>
  calculate_two_year_index()

so_far_years_1_7 <-
  dplyr::bind_rows(
    so_far_years_1_6,
    dplyr::slice(city_index_so_far, 7)
  ) |>
  calculate_two_year_index()

# Skipping intermediate years, adding just from first to last
city_index_so_far_all <-
  city_index_so_far |>
  dplyr::bind_rows(
    so_far_years_1_2,
    so_far_years_1_3,
    so_far_years_1_4,
    so_far_years_1_5,
    so_far_years_1_6,
    so_far_years_1_7
  ) |>
  dplyr::mutate(
    length_range = "lette",
    year_from_to = paste0(year_base, "-", year),
    area_name = "Trondheim",
    month_object = lubridate::make_date(year = year, month = month),
    period = "year_to_date",
    month_name =
      lubridate::month(month_object, label = TRUE, abbr = FALSE) |>
      stringr::str_to_title()
  )

readr::write_rds(
  city_index_so_far_all,
  file = paste0("data_indexpoints_tidy/city_index_so_far_", city_number, ".rds")
)


# Write Excel ----
source("city_index_to_excel.R")
