# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  library(tictoc)
  library(writexl)
}


# Tromsø 2019-2022 ----
## TRP ----
trp <- get_points()

trp_data_time_span <- get_trp_data_time_span()

trp_trs <-
  trp |>
  dplyr::filter(
    municipality_name == "Tromsø",
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id, name, road_reference
  ) |>
  dplyr::distinct() |>
  dplyr::left_join(
    trp_data_time_span,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    first_data_with_quality_metrics < "2019-02-01",
    latest_daily_traffic > "2022-12-01"
  )


## AADT to check coverage ----
aadt <- get_aadt_for_trp_list(trp_trs$trp_id)

aadt_tidy <-
  aadt |>
  dplyr::filter(
    year %in% c(2019, 2022)
  ) |>
  dplyr::select(
    trp_id, year, coverage
  )


## TRP ok ----
trp_trs_coverage <-
  trp_trs |>
  dplyr::left_join(
    aadt_tidy,
    by = join_by(trp_id)
  ) |>
  dplyr::filter(
    coverage > 50
  ) |>
  dplyr::summarise(
    n_years = n(),
    .by = c(trp_id, name, road_reference)
  ) |>
  dplyr::filter(
    n_years == 2
  ) |>
  dplyr::arrange(
    road_reference
  )


## Hourly data ----
calculate_hourly_index_traffic <- function(traffic_by_length_lane) {

  # IN: hourly traffic by length and lane
  # OUT: hourly traffic when all lanes present

  data_tidy <-
    traffic_by_length_lane |>
    tibble::as_tibble() |>
    dplyr::filter(
      length_range == "[..,5.6)",
      valid_length_percentage > 99,
      coverage > 95
    )

  lanes_each_month <-
    data_tidy |>
    dplyr::mutate(
      month = lubridate::month(from)
    ) |>
    dplyr::select(
      lane,
      month
    ) |>
    dplyr::distinct() |>
    dplyr::summarise(
      lanes_month = base::paste(lane, collapse = "#"),
      .by = month
    )

  hourly_traffic <-
    data_tidy |>
    dplyr::select(
      from,
      traffic,
      lane
    ) |>
    dplyr::summarise(
      lanes_hour = base::paste(lane, collapse = "#"),
      traffic = sum(traffic),
      .by = from
    ) |>
    dplyr::mutate(
      month = lubridate::month(from)
    ) |>
    dplyr::left_join(
      lanes_each_month,
      by = join_by(month)
    ) |>
    dplyr::filter(
      lanes_hour == lanes_month,
    ) |>
    dplyr::mutate(
      day = lubridate::day(from),
      hour = lubridate::hour(from)
    ) |>
    dplyr::select(
      month,
      day,
      hour,
      lanes_hour,
      traffic
    ) |>
    # Adding the two hours when DST is set back in October
    dplyr::summarise(
      traffic = sum(traffic),
      .by = c(tidyselect::everything(), -traffic)
    )

  return(hourly_traffic)
}


## TRP index ----
#trp_id <- "92719V1125906"
#base_year <- 2019
#calc_year <- 2022

calculate_trp_index <- function(trp_id, base_year, calc_year) {

  data_base_year <-
    get_hourly_traffic_by_length_lane(
      trp_id,
      paste0(as.character(base_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(base_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic()

  data_calc_year <-
    get_hourly_traffic_by_length_lane(
      trp_id,
      paste0(as.character(calc_year), "-01-01T00:00:00.000+01:00"),
      paste0(as.character(calc_year + 1), "-01-01T00:00:00.000+01:00")
    ) |>
    calculate_hourly_index_traffic()

  trp_index_data <-
    dplyr::inner_join(
      data_base_year,
      data_calc_year,
      by = join_by(month, day, hour, lanes_hour),
      suffix = c("_base", "_calc")
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_hours = n(),
      .by = c(month, day)
    ) |>
    dplyr::filter(
      n_hours >= 16
    ) |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_days = n(),
      .by = c(month)
    ) |>
    dplyr::filter(
      n_days >= 16
    ) |>
    dplyr::mutate(
      trp_id = trp_id,
      years = paste0(as.character(base_year), "-", as.character(calc_year))
    )

  readr::write_rds(
    trp_index_data,
    file = paste0("trp_index/", trp_id, "_", base_year, "_", calc_year, ".rds")
  )

  return(trp_index_data)

}

tictoc::tic()
trp_index_data <-
  calculate_trp_index(
    trp_trs_coverage$trp_id[15],
    2019,
    2022
  )
tictoc::toc()


## City index ----
all_trp_index_data <-
  base::list.files(path = "trp_index", full.names = TRUE) |>
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
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1)
  )

trp_index_meta_data <-
  all_trp_index_data |>
  dplyr::left_join(
    trp,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    index_p
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    name
  )

writexl::write_xlsx(
  trp_index_meta_data,
  "spesialuttak/trp_index_tromso.xlsx"
)

city_index_tromso <-
  all_trp_index_data |>
  dplyr::summarise(
    traffic_base = sum(traffic_base),
    traffic_calc = sum(traffic_calc),
    n_trp = n()
  ) |>
  dplyr::mutate(
    index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1)
  )

