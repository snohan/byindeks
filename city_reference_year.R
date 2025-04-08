reference_year <-
  dplyr::case_when(
    city_number %in% c(
      953,
      955,
      956,
      957,
      961,
      1952
    ) ~ 2016,
    city_number %in% c(
      952
    ) ~ 2017,
    city_number %in% c(
      959,
      8952
    ) ~ 2018,
    city_number %in% c(
      960
    ) ~ 2019,
    city_number %in% c(
      16952
    ) ~ 2022,
    city_number %in% c(
      18952,
      19953
    ) ~ 2023,
    city_number %in% c(
      19954,
      19955,
      20952
    ) ~ 2024
  )