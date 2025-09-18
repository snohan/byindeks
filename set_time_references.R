source("city_reference_year.R")

years_from_reference_to_today <- base::seq(reference_year, present_year)

# MDT periods ----
last_year_month <-
  lubridate::as_date(
    paste0(
      present_year,
      "-",
      index_month,
      "-01"
    )
  )

#if(!(city_number %in% c(960, 16952, 18952, 19953))){
if((city_number %in% c(1952, 955, 952, 959, 8952))){
  index_years_pre_2020 <- base::seq.int(reference_year + 1, 2019, 1)
}else{
  index_years_pre_2020 <- NULL
}

if(city_number == 8952){
  index_years_from_2020 <- base::seq.int(2019, present_year, 1)
}else{
  index_years_from_2020 <- base::seq.int(2020, present_year, 1)
}

if(city_number == 16952){
  index_years_from_2020 <- base::seq.int(2023, present_year, 1)
}

if(city_number %in% c(18952, 19953)){
  index_years_from_2020 <- base::seq.int(2024, present_year, 1)
}

if(city_number %in% c(19954, 19955, 20952)){
  index_years_from_2020 <- base::seq.int(2025, present_year, 1)
}

index_months_from_2020 <-
  c(
    base::rep(12, base::length(index_years_from_2020) - 1),
    index_month
  )

index_years <-
  base::seq.int(reference_year + 1, present_year, 1)

index_months <-
  c(
    base::rep(12, base::length(index_years) - 1),
    index_month
  )

latest_universal_year_period_id <-
  universal_calendar_periods |>
  dplyr::filter(
    year == present_year,
    month_id == index_month
  ) |>
  purrr::pluck(1)


# Two alternatives ----
## City specific ids ----
# last_period_id <-
#   period_names_df |>
#   dplyr::filter(period_name == chosen_period_name) |>
#   purrr::pluck(1)
#
# complete_period_names <- tibble::tibble()
#
# for(i in 1:length(years_from_reference_to_today)) {
#
#   add_year <-
#     period_names_df |>
#     dplyr::mutate(
#       year = years_from_reference_to_today[i]
#     )
#
#   complete_period_names <-
#     dplyr::bind_rows(complete_period_names, add_year)
# }

# Chip off periods later than the chosen
# all_period_names <-
#   complete_period_names |>
#   dplyr::filter(
#     !(year >= present_year & period_id > last_period_id)
#   ) |>
#   dplyr::mutate(
#     year_period_id = dplyr::row_number(),
#     year_period_name = base::paste0(year, "-", period_name)
#   )
