# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
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
  )


## Hourly data ----
trp_1 <- "80998V1125915"

data_1 <- get_hourly_traffic_by_length_lane(trp_1, "2019-01-01T00:00:00.00+01:00", "2020-01-01T00:00:00.000+01:00")

data_2 <- get_hourly_traffic_by_length_lane(trp_1, "2022-01-01T00:00:00.00+01:00", "2023-01-01T00:00:00.000+01:00")


## TRP index ----


## City index ----



