source("city_reference_year.R")

years_from_reference_to_today <- base::seq(reference_year, present_year)

# MDT periods
last_year_month <- lubridate::as_date(paste0(present_year, "-", index_month, "-01"))

if( city_number %in% c(1952, 955, 952, 959, 8952) ){
  index_years_pre_2020 <- base::seq.int(reference_year + 1, 2019, 1)
}else{
  index_years_pre_2020 <- NULL
}

if( city_number == 8952 ){
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

index_months_from_2020_so_far <- c(base::rep(index_month, base::length(index_years_from_2020)))

index_years <- base::seq.int(reference_year + 1, present_year, 1)

index_months <-
  c(
    base::rep(12, base::length(index_years) - 1),
    index_month
  )

index_months_so_far <-
  c(
    base::rep(index_month, base::length(index_years))
  )

latest_universal_year_period_id <-
  universal_calendar_periods |>
  dplyr::filter(
    year == present_year,
    month_id == index_month
  ) |>
  purrr::pluck(1)


if(index_month == 12) {
  relevant_years_to_plot <- c(reference_year, (present_year - 2):present_year)
}else{
  relevant_years_to_plot <- c(reference_year, (present_year - 3):present_year)
}

relevant_years_to_plot <- relevant_years_to_plot[relevant_years_to_plot >= reference_year]
