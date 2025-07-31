# Adjusting for calendar effects in xDT

# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  source("get_from_trafficdata_api.R")
  source("calendar_functions.R")
}

# Ascension Day ----
# How often is Ascension Day in June?
ascension_sundays_june <-
  purrr::map(
    c(2021:2050),
    #~ timeDate::Ascension(.x, +3) |> lubridate::as_date()
    ~ timeDate::Easter(.x, +42) |> lubridate::as_date()
  ) |>
  purrr::reduce(c) |>
  purrr::keep(stringr::str_detect, pattern = "-06-")

# Four times in 2021-2050
# "2025-06-01" "2030-06-02" "2038-06-06" "2041-06-02"
# CONCLUSION: Need not take this into account

# Number of non-working days per month ----

#year <- 2025
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

#test <- number_of_non_working_days(2025)

# find how the number varies by year in 2021:2050
n_non_w_days <-
  purrr::map(
    c(2021:2100),
    ~ number_of_non_working_days(.x)
  ) |>
  purrr::list_rbind() |>
  dplyr::summarise(
    mean_days = mean(n_days),
    median_days = median(n_days),
    .by = c(month, non_working_day)
  )

