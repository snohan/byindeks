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
# find how the number varies by year in 2021:2050
n_non_w_days_2 <-
  purrr::map(
    c(2016:2100),
    ~ number_of_non_working_days(.x)
  ) |>
  purrr::list_rbind() |>
  dplyr::summarise(
    mean_days = mean(n_days),
    mean_days_rounded = mean(n_days) |> round(3),
    median_days = median(n_days),
    .by = c(month, non_working_day)
  ) |>
  dplyr::mutate(
    n_days_month = sum(mean_days) |> round(2),
    .by = month
  )

# Definition of weights ----
period_days <-
  c(
    31,
    28.25,
    27.16,
    11,
    22.84,
    28.50,
    4,
    28.50,
    31,
    31,
    30,
    31,
    30,
    31
  )

periods <-
  tibble::tibble(
    period_name = base::factor(period_names, levels = period_names),
    period_days
  )

readr::write_rds(
  periods,
  "calendar_weights/periods.rds"
)

# Imputed period weights
period_weights_imputed <-
  dplyr::bind_rows(
    period_weights |>
      dplyr::filter(
        period_name %in% c("januar", "februar")
      ) |>
      dplyr::summarise(period_days = base::sum(period_days)) |>
      dplyr::mutate(month = "jan_feb"),
    period_weights |>
      dplyr::filter(
        period_name %in% c("mars", "april")
      ) |>
      dplyr::summarise(period_days = base::sum(period_days)) |>
      dplyr::mutate(month = "mar_apr"),
    period_weights |>
      dplyr::filter(
        period_name %in% c("september", "oktober", "november")
      ) |>
      dplyr::summarise(period_days = base::sum(period_days)) |>
      dplyr::mutate(month = "sep_okt_nov"),
    period_weights |>
      dplyr::filter(
        !(period_name %in% c("januar", "februar", "mars", "april", "september", "oktober", "november"))
      ) |>
      dplyr::rename(month = period_name)
  )

readr::write_rds(
  period_weights_imputed,
  "calendar_weights/period_weights_imputed.rds"
)

month_names <-
  c(
    "januar",
    "februar",
    "mars",
    "april",
    "mai",
    "juni",
    "juli",
    "august",
    "september",
    "oktober",
    "november",
    "desember"
  )

working_days <-
  c(
    21.41,
    20.18,
    19.70,
    16.88,
    18.57,
    20.65,
    22.14,
    22.14,
    21.44,
    22.13,
    21.44,
    19.31
  )

non_working_days <-
  c(
    9.59,
    8.07,
    7.46,
    5.96,
    9.93,
    7.85,
    8.86,
    8.86,
    8.56,
    8.87,
    8.56,
    11.69
  )

day_type_weights <-
  tibble::tibble(
    month_name = base::factor(month_names, levels = month_names),
    working_days,
    non_working_days
  )

readr::write_rds(
  day_type_weights,
  "calendar_weights/day_type_weights.rds"
)

day_type_weights_relative <-
  day_type_weights |>
  dplyr::mutate(
    sum_days = sum(working_days, non_working_days),
    working_day_weight = working_days / sum_days,
    non_working_day_weight = non_working_days / sum_days,
    .by = month_name
  ) |>
  dplyr::select(
    month = month_name,
    working_day = working_day_weight,
    non_working_day = non_working_day_weight
  ) |>
  tidyr::pivot_longer(
    cols = c(working_day, non_working_day),
    names_to = "non_working_day",
    values_to = "weight"
  ) |>
  dplyr::mutate(
    non_working_day =
      dplyr::case_when(
        non_working_day == "non_working_day" ~ TRUE,
        TRUE ~ FALSE
      )
  )

readr::write_rds(
  day_type_weights_relative,
  "calendar_weights/day_type_weights_relative.rds"
)


# Smaller uncertainty when aggregating YDT/HDT by weights ----



# Classification og TRPs by relative distribution of periods ----
# Is it important?