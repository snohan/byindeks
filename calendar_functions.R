library(timeDate)

# Day types ----
# Need to classify days as working days or not.
# Non-working days:
# All Saturdays and Sundays
# 1 January
# Maundy Thursday
# Good Friday
# Easter Monday
# 1 May
# 17 May
# Ascension day
# Pentecost Monday
# 24-26 December
# 31 December


find_moveable_holidays <- function(year) {

  c(
    timeDate::Easter(year, -3),
    timeDate::Easter(year, -2),
    timeDate::Easter(year,  1),
    timeDate::Ascension(year),
    timeDate::PentecostMonday(year)
  ) |>
  lubridate::as_date()

}


find_easter_days <- function(year) {

  base::seq.Date(
    timeDate::Easter(year, -9) |> lubridate::as_date(),
    timeDate::Easter(year, 1) |> lubridate::as_date(),
  )

}


find_pentecost_days <- function(year) {

  base::seq.Date(
    timeDate::Easter(year, 47) |> lubridate::as_date(),
    timeDate::Easter(year, 50) |> lubridate::as_date(),
  )

}


find_holidays <- function(year) {

  # Without Easter and Pentecost

  c(
    lubridate::make_date(year,  1,  1),
    lubridate::make_date(year,  5,  1),
    lubridate::make_date(year,  5, 17),
    timeDate::Ascension(year) |> lubridate::as_date(),
    lubridate::make_date(year, 12, 24),
    lubridate::make_date(year, 12, 25),
    lubridate::make_date(year, 12, 26),
    lubridate::make_date(year, 12, 31)
  )
}

number_of_non_working_days <- function(year_dbl) {

  dates <-
    base::seq.Date(
      lubridate::make_date(year_dbl,  1,  1),
      lubridate::make_date(year_dbl, 12, 31)
    )

  df <-
    tibble::tibble(
      date = dates
    ) |>
    dplyr::mutate(
      year = year_dbl,
      month = lubridate::month(date),
      weekday = lubridate::wday(date, week_start = 1)
    ) |>
    dplyr::filter(
      !(date %in% find_easter_days(year_dbl)),
      !(date %in% find_pentecost_days(year_dbl))
    ) |>
    dplyr::mutate(
      non_working_day =
        dplyr::case_when(
          weekday %in% c(6, 7) ~ TRUE,
          date %in% find_holidays(year_dbl) ~ TRUE,
          TRUE ~ FALSE
        )
    ) |>
    dplyr::summarise(
      n_days = n(),
      .by = c(year, month, non_working_day)
    )

}
