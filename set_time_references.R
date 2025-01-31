{
  source("city_reference_year.R")

  years_from_reference_to_today <-
    base::seq(reference_year, present_year)

  last_year_month <-
    lubridate::as_date(
      paste0(
        present_year,
        "-",
        index_month,
        "-01"
      )
    )

  if(!(city_number %in% c(960, 16952, 18952, 19953))){
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
}
