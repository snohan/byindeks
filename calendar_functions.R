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

period_names <-
  c(
    "januar",
    "februar",
    "mars",
    "påske",
    "april",
    "mai",
    "pinse",
    "juni",
    "juli",
    "august",
    "september",
    "oktober",
    "november",
    "desember"
  )

period_names_df <-
  tibble::tibble(
    period_id = c(1:14),
    period_name = period_names
  )


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


make_complete_calendar_year <- function(year) {

  # year: Int!

  easter_days <- find_easter_days(year)
  pentecost_days <- find_pentecost_days(year)

  df <-
    tibble::tibble(
      date =
        base::seq.Date(
          lubridate::make_date(year,  1,  1),
          lubridate::make_date(year, 12, 31)
        )
    ) |>
    dplyr::mutate(
      year = year,
      month = lubridate::month(date, label = TRUE, abbr = FALSE),
      weekday = lubridate::wday(date, week_start = 1),
      month =
        dplyr::case_when(
          date %in% easter_days ~ "påske",
          date %in% pentecost_days ~ "pinse",
          TRUE ~ month
        ),
      month = base::factor(month, levels = period_names)
    )

}

#complete_calendar_test <- make_complete_calendar_year(2025)


number_of_days <- function(year) {

  # year: Int!

  df <-
    make_complete_calendar_year(year) |>
    dplyr::summarise(
      n_days = n(),
      .by = month
    )

}

#n_days_test <- number_of_days(2025)


classify_days <- function(year) {

  holidays <- find_holidays(year)

  df <-
    make_complete_calendar_year(year) |>
    dplyr::filter(
      !(month %in% c("påske", "pinse"))
    ) |>
    dplyr::mutate(
      non_working_day =
        dplyr::case_when(
          weekday %in% c(6, 7) ~ TRUE,
          date %in% holidays ~ TRUE,
          TRUE ~ FALSE
        )
    )

}

#classified_days_test <- classify_days(2025)


number_of_non_working_days <- function(year) {

  classify_days(year) |>
    dplyr::summarise(
      n_days = n(),
      .by = c(year, month, non_working_day)
    )

}

#number_of_non_working_days_test <- number_of_non_working_days(2025)